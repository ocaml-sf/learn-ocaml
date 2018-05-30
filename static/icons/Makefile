ICONS = \
  $(patsubst icon_%_src.svg, icon_%_black.svg, $(wildcard icon_*.svg)) \
  $(patsubst icon_%_src.svg, icon_%_white.svg, $(wildcard icon_*.svg)) \
  $(patsubst icon_%_src.svg, icon_%_dark.svg, $(wildcard icon_*.svg)) \
  $(patsubst icon_%_src.svg, icon_%_light.svg, $(wildcard icon_*.svg))

all: ${ICONS}

icon_%_dark.svg: icon_%_src.svg
	sed "s/#000000/#222/g" $< > $@
icon_%_light.svg: icon_%_src.svg
	sed "s/#000000/#EEE/g" $< > $@
icon_%_white.svg: icon_%_src.svg
	sed "s/#000000/#FFF/g" $< > $@
icon_%_black.svg: icon_%_src.svg
	sed "s/#000000/#000/g" $< > $@

clean:
	-rm -f icon_*_dark.svg
	-rm -f icon_*_light.svg
	-rm -f icon_*_white.svg
	-rm -f icon_*_black.svg
