.PHONY: clean
clean:
	@i=1 ; while [[ $$i -le 25 ]] ; do \
	  rm -f "day"$$i/"day"$$i ; \
	  rm -f "day"$$i/*.hi ; \
	  rm -f "day"$$i/*.o ; \
	  (( i = i + 1 )) ; \
	done
