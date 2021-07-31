icons.out: icons.R
	Rscript $< > $@
install: force
	cp *png ../../R/www
clean: force
	rm *~ pch_*.png *.out
force:

