use super::{
	roc_per_second, AccountId, Balance, Balances, BlockNumber, Call, Currencies, Event, Origin,
	ParachainInfo, ParachainSystem, PolkadotXcm, Runtime, Tokens, TreasuryPalletId, UnknownTokens,
	Vec, XcmpQueue,
};
use codec::{Decode, Encode};
use frame_support::{
	match_type, parameter_types,
	traits::{Everything, Nothing},
	weights::Weight,
};
use frame_system::EnsureRoot;
use pallet_xcm::XcmPassthrough;
use polkadot_parachain::primitives::Sibling;
#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};

use sp_runtime::{
	traits::{AccountIdConversion, Convert},
	RuntimeDebug,
};

use xcm::latest::prelude::*;
use xcm_builder::{
	AccountId32Aliases, AllowTopLevelPaidExecutionFrom, AllowUnpaidExecutionFrom, EnsureXcmOrigin,
	FixedRateOfFungible, FixedWeightBounds, LocationInverter, ParentIsPreset, RelayChainAsNative,
	SiblingParachainAsNative, SiblingParachainConvertsVia, SignedAccountId32AsNative,
	SignedToAccountId32, SovereignSignedViaLocation, TakeWeightCredit,
};

use scale_info::TypeInfo;
use xcm_executor::XcmExecutor;

use orml_currencies::BasicCurrencyAdapter;
use orml_traits::parameter_type_with_key;
use orml_xcm_support::{
	DepositToAlternative, IsNativeConcrete, MultiCurrencyAdapter, MultiNativeAsset,
};

parameter_types! {
	pub const RelayLocation: MultiLocation = MultiLocation::parent();
	pub const RelayNetwork: NetworkId = NetworkId::Any;
	pub RelayChainOrigin: Origin = cumulus_pallet_xcm::Origin::Relay.into();
	pub Ancestry: MultiLocation = Parachain(ParachainInfo::parachain_id().into()).into();
}

/// Type for specifying how a `MultiLocation` can be converted into an `AccountId`. This is used
/// when determining ownership of accounts for asset transacting and when attempting to use XCM
/// `Transact` in order to determine the dispatch Origin.
pub type LocationToAccountId = (
	// The parent (Relay-chain) origin converts to the parent `AccountId`.
	ParentIsPreset<AccountId>,
	// Sibling parachain origins convert to AccountId via the `ParaId::into`.
	SiblingParachainConvertsVia<Sibling, AccountId>,
	// Straight up local `AccountId32` origins just alias directly to `AccountId`.
	AccountId32Aliases<RelayNetwork, AccountId>,
);

/// This is the type we use to convert an (incoming) XCM origin into a local `Origin` instance,
/// ready for dispatching a transaction with Xcm's `Transact`. There is an `OriginKind` which can
/// biases the kind of local `Origin` it will become.
pub type XcmOriginToTransactDispatchOrigin = (
	// Sovereign account converter; this attempts to derive an `AccountId` from the origin location
	// using `LocationToAccountId` and then turn that into the usual `Signed` origin. Useful for
	// foreign chains who want to have a local sovereign account on this chain which they control.
	SovereignSignedViaLocation<LocationToAccountId, Origin>,
	// Native converter for Relay-chain (Parent) location; will converts to a `Relay` origin when
	// recognized.
	RelayChainAsNative<RelayChainOrigin, Origin>,
	// Native converter for sibling Parachains; will convert to a `SiblingPara` origin when
	// recognized.
	SiblingParachainAsNative<cumulus_pallet_xcm::Origin, Origin>,
	// Native signed account converter; this just converts an `AccountId32` origin into a normal
	// `Origin::Signed` origin of the same 32-byte value.
	SignedAccountId32AsNative<RelayNetwork, Origin>,
	// Xcm origins can be represented natively under the Xcm pallet's Xcm origin.
	XcmPassthrough<Origin>,
);

parameter_types! {
	// One XCM operation is 1_000_000_000 weight - almost certainly a conservative estimate.
	pub UnitWeightCost: Weight = 1_000_000_000;
	pub const MaxInstructions: u32 = 100;
}

match_type! {
	pub type ParentOrParentsExecutivePlurality: impl Contains<MultiLocation> = {
		MultiLocation { parents: 1, interior: Here } |
		MultiLocation { parents: 1, interior: X1(Plurality { id: BodyId::Executive, .. }) }
	};
}

match_type! {
	pub type SpecParachain: impl Contains<MultiLocation> = {
		// MultiLocation {parents: 1, interior: X1(Parachain(1000))} |
		MultiLocation {parents: 1, interior: X1(Parachain(2115))} |
		MultiLocation {parents: 1, interior: X1(Parachain(3000))}
	};
}

pub type Barrier = (
	TakeWeightCredit,
	AllowTopLevelPaidExecutionFrom<Everything>,
	AllowUnpaidExecutionFrom<ParentOrParentsExecutivePlurality>,
	// ^^^ Parent and its exec plurality get free execution
	AllowUnpaidExecutionFrom<SpecParachain>,
);

pub type LocalAssetTransactor = MultiCurrencyAdapter<
	Currencies,
	UnknownTokens,
	IsNativeConcrete<CurrencyId, CurrencyIdConvert>,
	AccountId,
	LocationToAccountId,
	CurrencyId,
	CurrencyIdConvert,
	DepositToAlternative<NativeTreasuryAccount, Currencies, CurrencyId, AccountId, Balance>,
>;

/// Trader - The means of purchasing weight credit for XCM execution.
/// We need to ensure we have at least one rule per token we want to handle or else
/// the xcm executor won't know how to charge fees for a transfer of said token.
pub type Trader = (
	FixedRateOfFungible<RocPerSecond, ()>,
	FixedRateOfFungible<NativePerSecond, ()>,
	FixedRateOfFungible<NativeNewPerSecond, ()>,
	// FixedRateOfFungible<FfPerSecond, ()>,
	FixedRateOfFungible<DoraPerSecond, ()>,
);

parameter_types! {
	pub RocPerSecond: (AssetId, u128) = (MultiLocation::parent().into(), roc_per_second());
	pub NativePerSecond: (AssetId, u128) = (
		MultiLocation::new(
			1,
			X2(Parachain(3000), GeneralKey(b"DD".to_vec()))
		).into(),
		// DD:ROC = 80:1
		roc_per_second() * 80
		// 10_000
	);
	pub NativeNewPerSecond: (AssetId, u128) = (
		MultiLocation::new(
			0,
			X1(GeneralKey(b"DD".to_vec()))
		).into(),
		// DD:ROC = 80:1
		roc_per_second() * 80
		// 10_000
	);

	// pub FfPerSecond: (AssetId, u128) = (
	// 	MultiLocation::new(
	// 		1,
	// 		X2(Parachain(1000), GeneralKey(b"FF".to_vec()))
	// 	).into(),
	// 	// FF:ROC = 100:1
	// 	roc_per_second() * 100
	// );

	pub DoraPerSecond: (AssetId, u128) = (
		MultiLocation::new(
			1,
			X2(Parachain(2115), GeneralKey(b"DORA".to_vec()))
		).into(),
		// DORA:ROC = 100:1
		roc_per_second() * 100
	);
}

pub struct XcmConfig;
impl xcm_executor::Config for XcmConfig {
	type Call = Call;
	type XcmSender = XcmRouter;
	// How to withdraw and deposit an asset.
	type AssetTransactor = LocalAssetTransactor;
	type OriginConverter = XcmOriginToTransactDispatchOrigin;
	type IsReserve = MultiNativeAsset;
	type IsTeleporter = (); // Teleporting is disabled.
	type LocationInverter = LocationInverter<Ancestry>;
	type Barrier = Barrier;
	type Weigher = FixedWeightBounds<UnitWeightCost, Call, MaxInstructions>;
	type Trader = Trader;
	type ResponseHandler = PolkadotXcm;
	type AssetTrap = PolkadotXcm;
	type AssetClaims = PolkadotXcm;
	type SubscriptionService = PolkadotXcm;
}

/// No local origins on this chain are allowed to dispatch XCM sends/executions.
pub type LocalOriginToLocation = SignedToAccountId32<Origin, AccountId, RelayNetwork>;

/// The means for routing XCM messages which are not for local execution into the right message
/// queues.
pub type XcmRouter = (
	// Two routers - use UMP to communicate with the relay chain:
	cumulus_primitives_utility::ParentAsUmp<ParachainSystem, PolkadotXcm>,
	// ..and XCMP to communicate with the sibling chains.
	XcmpQueue,
);

impl pallet_xcm::Config for Runtime {
	type Event = Event;
	type SendXcmOrigin = EnsureXcmOrigin<Origin, LocalOriginToLocation>;
	type XcmRouter = XcmRouter;
	type ExecuteXcmOrigin = EnsureXcmOrigin<Origin, LocalOriginToLocation>;
	type XcmExecuteFilter = Nothing;
	// ^ Disable dispatchable execute on the XCM pallet.
	// Needs to be `Everything` for local testing.
	type XcmExecutor = XcmExecutor<XcmConfig>;
	type XcmTeleportFilter = Everything;
	type XcmReserveTransferFilter = Nothing;
	type Weigher = FixedWeightBounds<UnitWeightCost, Call, MaxInstructions>;
	type LocationInverter = LocationInverter<Ancestry>;
	type Origin = Origin;
	type Call = Call;

	const VERSION_DISCOVERY_QUEUE_SIZE: u32 = 100;
	// ^ Override for AdvertisedXcmVersion default
	type AdvertisedXcmVersion = pallet_xcm::CurrentXcmVersion;
}

impl cumulus_pallet_xcm::Config for Runtime {
	type Event = Event;
	type XcmExecutor = XcmExecutor<XcmConfig>;
}

parameter_types! {
	pub const GetNativeCurrencyId: CurrencyId = CurrencyId::DD;
}

impl orml_currencies::Config for Runtime {
	type Event = Event;
	type MultiCurrency = Tokens;
	type NativeCurrency = BasicCurrencyAdapter<Runtime, Balances, Amount, BlockNumber>;
	type GetNativeCurrencyId = GetNativeCurrencyId;
	type WeightInfo = ();
}

// orml_xtokens
#[derive(
	Encode,
	Decode,
	Eq,
	PartialEq,
	Copy,
	Clone,
	RuntimeDebug,
	PartialOrd,
	Ord,
	codec::MaxEncodedLen,
	TypeInfo,
)]
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
pub enum CurrencyId {
	// / Relay chain token.
	ROC,
	// Native TokenSymbol
	DD,
	// Parachain A token.
	// FF,
	// Parachain B token.
	DORA,
}

pub type Amount = i128;

pub struct CurrencyIdConvert;
impl Convert<CurrencyId, Option<MultiLocation>> for CurrencyIdConvert {
	fn convert(id: CurrencyId) -> Option<MultiLocation> {
		match id {
			CurrencyId::ROC => Some(Parent.into()),
			CurrencyId::DD => Some((Parent, Parachain(3000), GeneralKey("DD".into())).into()),
			// CurrencyId::FF => Some((Parent, Parachain(1000), GeneralKey("FF".into())).into()),
			CurrencyId::DORA => Some((Parent, Parachain(2115), GeneralKey("DORA".into())).into()),
			// _ => {},
		}
	}
}

impl Convert<MultiLocation, Option<CurrencyId>> for CurrencyIdConvert {
	fn convert(l: MultiLocation) -> Option<CurrencyId> {
		let dd: Vec<u8> = "DD".into();
		// let ff: Vec<u8> = "FF".into();
		let dora: Vec<u8> = "DORA".into();
		if l == MultiLocation::parent() {
			return Some(CurrencyId::ROC);
		}

		match l {
			MultiLocation { parents, interior } if parents == 1 => match interior {
				X2(Parachain(3000), GeneralKey(k)) if k == dd => Some(CurrencyId::DD),
				// X2(Parachain(1000), GeneralKey(k)) if k == ff => Some(CurrencyId::FF),
				X2(Parachain(2115), GeneralKey(k)) if k == dora => Some(CurrencyId::DORA),
				_ => None,
			},
			MultiLocation { parents, interior } if parents == 0 => match interior {
				X1(GeneralKey(k)) if k == dd => Some(CurrencyId::DD),
				// X1(GeneralKey(k)) if k == ff => Some(CurrencyId::FF),
				X1(GeneralKey(k)) if k == dora => Some(CurrencyId::DORA),
				_ => None,
			},
			_ => None,
		}
	}
}

impl Convert<MultiAsset, Option<CurrencyId>> for CurrencyIdConvert {
	fn convert(asset: MultiAsset) -> Option<CurrencyId> {
		if let MultiAsset {
			// fun: Fungible(_),
			id: Concrete(id),
			..
		} = asset
		{
			Self::convert(id)
		} else {
			Option::None
		}
	}
}

pub struct AccountIdToMultiLocation;
impl Convert<AccountId, MultiLocation> for AccountIdToMultiLocation {
	fn convert(account: AccountId) -> MultiLocation {
		X1(AccountId32 { network: NetworkId::Any, id: account.into() }).into()
	}
}

parameter_types! {
	pub SelfLocation: MultiLocation = MultiLocation::new(1, X1(Parachain(ParachainInfo::parachain_id().into())));
	pub const BaseXcmWeight: Weight = 100_000_000;
	pub const MaxAssetsForTransfer: usize = 2;
}

parameter_type_with_key! {
	pub ParachainMinFee: |location: MultiLocation| -> u128 {
		#[allow(clippy::match_ref_pats)] // false positive
		match (location.parents, location.first_interior()) {
			(1, Some(Parachain(3000))) => 4_000_000_000,
			_ => u128::MAX,
		}
	};
}

impl orml_xtokens::Config for Runtime {
	type Event = Event;
	type Balance = Balance;
	type CurrencyId = CurrencyId;
	type CurrencyIdConvert = CurrencyIdConvert;
	type AccountIdToMultiLocation = AccountIdToMultiLocation;
	type SelfLocation = SelfLocation;
	type MinXcmFee = ParachainMinFee;
	type XcmExecutor = XcmExecutor<XcmConfig>;
	type Weigher = FixedWeightBounds<UnitWeightCost, Call, MaxInstructions>;
	type BaseXcmWeight = BaseXcmWeight;
	type LocationInverter = LocationInverter<Ancestry>;
	type MaxAssetsForTransfer = MaxAssetsForTransfer;
}

parameter_type_with_key! {
	pub ExistentialDeposits: |currency_id: CurrencyId| -> Balance {
		// every currency has a zero existential deposit
		match currency_id {
			_ => 0,
		}
	};
}

parameter_types! {
	pub ORMLMaxLocks: u32 = 2;
	pub NativeTreasuryAccount: AccountId = TreasuryPalletId::get().into_account();

}

impl orml_tokens::Config for Runtime {
	type Event = Event;
	type Balance = Balance;
	type Amount = Amount;
	type CurrencyId = CurrencyId;
	type WeightInfo = ();
	type ExistentialDeposits = ExistentialDeposits;
	// type OnDust = orml_tokens::TransferDust<Runtime, NativeTreasuryAccount>;
	type OnDust = ();
	type MaxLocks = ORMLMaxLocks;
	type DustRemovalWhitelist = Nothing;
}

// orml unknown tokens
impl orml_unknown_tokens::Config for Runtime {
	type Event = Event;
}

impl orml_xcm::Config for Runtime {
	type Event = Event;
	type SovereignOrigin = EnsureRoot<AccountId>;
}
