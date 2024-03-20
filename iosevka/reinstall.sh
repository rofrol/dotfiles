cd ~/personal_projects/vendor/Iosevka &&
	npm run build -- ttf::IosevkaCustom &&
	cd ~/personal_projects/vendor/FontPatcher &&
	fontforge -script font-patcher --complete ~/personal_projects/vendor/Iosevka/dist/IosevkaCustom/TTF/IosevkaCustom-Regular.ttf &&
	cp IosevkaCustomNerdFont-Regular.ttf ~/iosevka/ &&
	open ~/iosevka/IosevkaCustomNerdFont-Regular.ttf
