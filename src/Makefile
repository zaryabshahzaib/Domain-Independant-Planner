# proj01
#
# This Makefile is provided to help you organize the files
# for submission. It comes with ABSOLUTELY NO WARRANY and you
# are ultimately responsible for the files you submit.

.PHONY: all run clean submit
all: proj01.pdf proj01-handin.tgz

proj01-handin.tgz: planner.sml test.sml 
	tar -czf proj01-handin.tgz Makefile $^

# "I accidentally submitted old LaTeX..."  Don't be that guy.
%.pdf: latex/%-template.tex
	echo "Recompiling LaTeX..."; cd latex; pwd; pdflatex -jobname=$* $*-template.tex; cd ..

clean:
	rm -f proj01-handin.tgz
	rm -f proj01.pdf
	rm -Rf .cm

