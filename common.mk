################################################################
# Org-lectures Options
LATEXMK ?= latexmk -pdf -lualatex -recorder -outdir=build 
OL_DIR  ?= org-lecture
export OL_DIR
VIEW    ?= xdg-open

## Derived Variables
OL_TOOLS := ${OL_DIR}/tools

SLIDE_TEX_DEPENDS += preamble.tex ${OL_DIR}/preamble.tex

MAKEFLAGS += --no-builtin-rules

all: build
htm: build


################################################################
# Build Directory Setup
build:
	@echo "[org-lecture] Setup build directory and copy HTML files."
	@mkdir -p build/tangle
	@mkdir -p build/html/css build/html/img build/html/js
	@rsync -a ${OL_DIR}/html/. build/html/.
	@if [ -x html ]; then rsync -a html/. build/html/.; fi
	@ln -fs ../export-prologue.org build
	@if [ -x ./fig ]; then ln -fs ../../fig     build/html; fi
	@if [ -x ./lst ]; then ln -fs ../../lst     build/html; fi


ARTIFACTS += build

################################################################
# Emacs Server
EMACS_SESSION ?= org-lecture
EC=make -s emacs/ensure; emacsclient -s ${EMACS_SESSION}
EMACS_STAMP=build/.emacs.stamp

emacs/start: ${EMACS_STAMP}

${EMACS_STAMP}:
	emacs -q -l ${OL_DIR}/site-lisp/init.el --daemon=${EMACS_SESSION}
	@mkdir -p build
	@touch ${EMACS_STAMP}

emacs/debug:
	@emacs --debug-init -q -l ${OL_DIR}/site-lisp/init.el

emacs/stop:
	@emacsclient -s ${EMACS_SESSION}  -e '(kill-emacs 0)' >/dev/null 2>&1 || true
	@pgrep -l -f "^emacs.*--daemon=${EMACS_SESSION}" || true
	@pkill -f "^emacs.*--daemon=${EMACS_SESSION}" || true
	@rm -f ${EMACS_STAMP}

emacs/restart: emacs/stop emacs/start

emacs/ensure:
	@emacsclient -s ${EMACS_SESSION} -e 't' >/dev/null 2>&1 || make -s emacs/restart

################################################################
# Make tooling

define invoke
# $(1): macro name (e.g., PROCESS_SLIDE)
# $(2): list of org-mode files
$(foreach file,$(2),\
$(call $(1),$(shell echo $(file:.org=) | awk -F- '{print $$1;}'),$(file:.org=)))
endef

HELP=""
help:
	@echo -n ${HELP} | sort -n

################################################################
# Slide Processing (org -> tex -> pdf)
define PROCESS_SLIDE # $(1) = 01, $(2) = 01-einleitung

build/tangle/$(2).tex: $(2).org ${EMACS_STAMP}
	@mkdir -p build/tangle
	${EC} -e '(org-babel-tangle-file "${PWD}/$(2).org" nil "latex")'
	@mv $(2).tex $$@
	${OL_TOOLS}/delete-frames $$@

# Slides
build/$(2).slides.tex: build/tangle/$(2).tex
	$${OL_TOOLS}/gen-latex-root beamer $$< > $$@

build/$(2).slides.pdf: build/$(2).slides.tex ${SLIDE_TEX_DEPENDS}
	${LATEXMK} $$< -deps-out=build/$(2).slides.deps
	@mkdir -p build/html; cp $$@ build/html

-include build/$(2).slides.deps

slides: build/$(2).slides.pdf

HELP+="$(1):               Build slides: $(2).org\n"
$(1): build/$(2).slides.pdf

HELP+="$(1).view:          View slides: $(2).org\n"
$(1).view: build/$(2).slides.pdf
$(1).all: build/$(2).slides.pdf


# Handouts.
build/$(2).handout.tex: build/tangle/$(2).tex
	@$${OL_TOOLS}/gen-latex-root handout $$< > $$@

build/$(2).handout.pdf: build/$(2).handout.tex ${SLIDE_TEX_DEPENDS}
	${LATEXMK} $$< -deps-out=build/$(2).handout.deps
	@mkdir -p build/html; cp $$@ build/html

-include build/$(2).handout.deps

HELP+="$(1).handout:       Build handout from $(2).org\n"
$(1).handout: build/$(2).handout.pdf

HELP+="$(1).handout.view:  View handout: $(2).org\n"
$(1).handout.view: build/$(2).handout.pdf
$(1).all: build/$(2).handout.pdf
all: $(1).all


handouts: build/$(2).handout.pdf

# Add figures as dependencies
build/$(2).slides.pdf build/$(2).handout.pdf: $(shell find fig/ -name "$(1)-*.pdf")

endef

# Invoke macro on ORG_SLIDES list
$(eval $(call invoke,PROCESS_SLIDE,$(ORG_SLIDES)))

################################################################
# Create HTML Files from SLIDES

define PROCESS_SLIDE_HTML
# $(1) = 01
# $(2) = 01-einleitung
build/html/$(2).handout/.split-stamp: build/$(2).handout.pdf $(OL_TOOLS)/split-pdf
	@mkdir -p build/html/$(2).handout/
	@rm -f build/html/$(2)/*.pdf build/html/$(2)/*.svg
	@${OL_TOOLS}/split-pdf $$< 
	@touch $$@

HELP+="$(1).handout:       Split handout into svg pieces\n"
$(1).split: build/html/$(2).handout/.split-stamp

build/$(2).org: $(2).org build/html/$(2).handout/.split-stamp $${TOPIC_FILES_$(1)} $${TOPIC_FILES_$(2)} ${OL_TOOLS}/insert-carousels
	@mkdir -p build
	${OL_TOOLS}/insert-carousels $(2).org build/$(2).handout.topics $${TOPIC_FILES_$(1)} $${TOPIC_FILES_$(2)} > $$@

build/html/$(2).html: build/$(2).org ${OL_TOOLS}/html-postprocess ${EMACS_STAMP}
	@mkdir -p build/html
	${EC} -e '(org-export-to-html-file "${PWD}/build/$(2).org" "${PWD}/build/html/$(2).html")'
	${OL_TOOLS}/html-postprocess build/html/$(2).html

HELP+="$(1).html:       Build HTML file\n"
$(1).html: build build/html/$(2).html
$(1).all: build build/html/$(2).html

HELP+="$(1).html.view:       View HTML file\n"
$(1).html.view: build/html/$(2).html

endef

$(eval $(call invoke,PROCESS_SLIDE_HTML,$(ORG_SLIDES)))


################################################################
# Direct Org->HTML Export
define PROCESS_HTML
# $(1): basename
# $(2): basename

build/html/$(2).html: build/$(2).org
	@mkdir -p build/html
	${EC} -e '(org-export-to-html-file "${PWD}/build/$(2).org" "${PWD}/build/html/$(2).html")'
	${OL_TOOLS}/html-postprocess build/html/$(2).html

build/$(2).org: $(2).org ${OL_TOOLS}/insert-carousels $${TOPIC_FILES_$(1)} $${TOPIC_FILES_$(2)}
	@mkdir -p build
	${OL_TOOLS}/insert-carousels $(2).org $${TOPIC_FILES_$(1)} $${TOPIC_FILES_$(2)} > $$@

html: build/html/$(2).html
$(1).html: build/html/$(2).html
$(1).html.view: build/html/$(2).html
$(1).view: build/html/$(2).html
$(1): build/html/$(2).html
$(1).all: build/html/$(2).html
all: $(1).all

HELP+="$(1).html:       Build HTML file\n"
HELP+="$(1).html.view:  View HTML file\n"

endef

$(eval $(call invoke,PROCESS_HTML,$(ORG_HTML)))

################################################################
# Add an external PDF as to an HTML target
define EXTERNAL_PDF
# $(1): topic-prefix
# $(2): PDF
# $$(info EXTERNAL_PDF: $(1), $(2))

build/$(1).topics: $(2) build/html/$(1)/slide-0001.svg ${OL_TOOLS}/bookmark-topics
	${OL_TOOLS}/bookmark-topics $(2) $(1) > $$@

build/html/$(1)/slide-0001.svg: $(2) ${OL_TOOLS}/split-pdf
	${OL_TOOLS}/split-pdf $(2) $(1)
	@cp $(2) build/html/

endef

################################################################
# Stamp file support for index Page
define PROCESS_STAMP
# $(1): chapter (e.g., 01)
# $(2): base file
# $(3): page
fig/$(1)-stamp.png: $(2)
	convert -density 300 $(2)[$(3)]  -quality 100 -geometry 200x150 $$@
index.html: fig/$(1)-stamp.png
endef

# Example: $(eval $(call PROCESS_STAMP,09,fig/09-optimization-depends.pdf,0))

################################################################
# PDF pictures for thml script
define PROCESS_PDF2PNG
# $(1): PDF file
# $(2): Page
$(patsubst %.pdf, %-$(2).png, $(1)): $(1)
	convert -density 300 $$<[$(2)] -quality 100 -geometry 800x600 $$@
endef

################################################################
# Wordcount support
define PROCESS_WORDCOUNT
# $(1) = 01
# $(2) = 01-einleitung
ifneq "$(1)" "$(2)"
$(1).wc: $(2).wc
endif
$(2).wc:
	@awk 'BEGIN {IGNORECASE=1; p=1}; /#\+begin_(example|src)/ {p=0}; {if(p) print};  /#\+end_(example|src)/ {p=1}' < $(2).org | wc -w
WC+=$(2).wc

endef

$(eval $(call invoke,PROCESS_WORDCOUNT,$(ORG_SLIDES)))
$(eval $(call invoke,PROCESS_WORDCOUNT,$(ORG_HTML)))

wc:
	@make -s ${WC} | tr "\n" " " | dc -f - -e '[+z1<r]srz1<rp'

################################################################
# Publish files to REMOTE
define PROCESS_PUBLISH
# $(1) = 01
# $(2) = 01-einleitung
$(1).publish: build build/html/index.html $(1).all
	cd build/html; rsync -aLv  ./img ./css ./js ./lst ./fig ./$(2)* $${REMOTE}

publish: $(1).publish
endef

$(eval $(call invoke,PROCESS_PUBLISH,$(ORG_SLIDES)))
$(eval $(call invoke,PROCESS_PUBLISH,$(ORG_HTML)))


################################################################
# Wildcard rules
################################################################

# View targets
%.view:
	${VIEW} $< &

################################################################
# Figures
fig/%.pdf: fig/%.tex ${FIGURE_TEX_DEPENDS}
	@${MAKE} build
	latexmk -pdf $< -outdir=build
	@cp $(patsubst fig/%,build/%,$@) $@

fig/%.pdf: fig/%.dot
	@${MAKE} build
	dot -Tpdf $< > $@

fig/%.pdf: fig/%.svg ${OL_TOOLS}/svgfig
	${OL_TOOLS}/svgfig $<

ifdef TEXMF
build: ${TEXMF}/ls-R

${TEXMF}/ls-R:
	mktexlsr ${TEXMF}

ARTIFACTS += ../texmf/ls-R

export TEXINPUTS := ${TEXMF}//:${TEXINPUTS}
endif


clean_common: emacs/stop
	rm -rf ${ARTIFACTS}

clean: clean_common

.PRECIOUS: build/%.pdf build/tangle/%.tex build/%.pdf.split build/%.tex
.PHONY:  build
