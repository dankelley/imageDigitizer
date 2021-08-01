icons.out: icons.R
	Rscript $< > $@
install: force
	cp *png ../../inst/shiny/imageDigitizer/www
clean: force
	rm *~ pch_*.png *.out
force:

