.PHONY: all
all:
	@cd src && $(MAKE) byte-code 

.PHONY: native
native:
	@cd src && $(MAKE) native-code 


.PHONY: debug
debug:
	@cd src && $(MAKE) debug-code


.PHONY:	clean
clean:
	@cd src && $(MAKE) clean

