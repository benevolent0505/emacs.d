OUTPUT := $(shell emacs -batch -l init.el 2>&1 | tail -1)

.PHONY: check

check:
ifeq ($(OUTPUT),End of loading init.el.)
	echo "Success!";
else
	echo "Failure!";
	exit 1;
endif

