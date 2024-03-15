# iosevka

generate `private-build-plans.toml` at https://typeof.net/Iosevka/customizer

- Overview of stylistic sets https://github.com/be5invis/Iosevka/blob/main/doc/stylistic-sets.md
  - https://news.ycombinator.com/item?id=36785409
  - https://github.com/aaronmbos/monocode/blob/main/src/private-build-plans.toml
  - https://github.com/shytikov/pragmasevka/blob/main/private-build-plans.toml
  - https://git.sr.ht/~ashton314/iosevka-output/tree/main/item/private-build-plans.toml

```shell
brew install ttfautohint fontforge
git clone --depth 1 https://github.com/be5invis/Iosevka.git
cd Iosevka
ln -s ~/iosevka/private-build-plans.toml .
npm install
npm run build -- contents::IosevkaCustom
cd ..
curl -OJNL https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FontPatcher.zip
unzip FontPatcher.zip -d FontPatcher
cd FontPatcher
fontforge -script font-patcher --complete ~/personal_projects/vendor/Iosevka/dist/IosevkaCustom/TTF/IosevkaCustom-Regular.ttf
cp IosevkaCustomNerdFont-Regular.ttf ~/iosevka/
open ~/iosevka/IosevkaCustomNerdFont-Regular.ttf
```

`./reinstall.sh`
