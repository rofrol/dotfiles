# iosevka

generate `private-build-plans.toml` at https://typeof.net/Iosevka/customizer

```shell
brew install ttfautohint fontforge
git clone --depth 1 https://github.com/be5invis/Iosevka.git
cd Iosevka
cp path/to/private-build-plans.toml .
npm install
npm run build -- contents::IosevkaCustom
cd ..
curl -OJNL https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FontPatcher.zip
unzip FontPatcher.zip -d FontPatcher
cd FontPatcher
fontforge -script font-patcher --complete path/to/Iosevka/dist/IosevkaCustom/TTF/IosevkaCustom-Regular.ttf
open IosevkaCustomNerdFont-Regular.ttf
```
