#!/bin/bash
echo "正在生成chain spec1..."
./target/release/parachain-collator build-spec --disable-default-bootnode > rococo-local-parachain-plain.json
echo "正在生成chain spec2..."
./target/release/parachain-collator build-spec --chain rococo-local-parachain-plain.json --raw --disable-default-bootnode > rococo-local-parachain-3000-raw.json
echo "正在生成genesis..."
./target/release/parachain-collator export-genesis-state --chain rococo-live-parachain-2077-raw.json > para-2077-genesis
echo "正在生成wasm..."
./target/release/parachain-collator export-genesis-wasm --chain rococo-live-parachain-2077-raw.json > para-2077-wasm
echo "完成！"
