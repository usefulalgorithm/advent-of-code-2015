.PHONY: clean

clean:
	@for i in $$(seq 1 25); do \
	  rm -f "day"$$i/"day"$$i ; \
	  rm -f "day"$$i/*.hi ; \
	  rm -f "day"$$i/*.o ; \
	done
