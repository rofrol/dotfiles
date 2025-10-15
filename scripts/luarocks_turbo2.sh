# https://glebovdev.medium.com/fixing-crypto-errors-with-luarocks-on-macos-an-easy-fix-54dc49ca5abc
# https://github.com/kernelsauce/turbo/issues/362
luarocks install turbo CRYPTO_INCDIR=$(brew --prefix openssl)/include/ OPENSSL_DIR=$(brew --prefix openssl)/include/openssl --local
