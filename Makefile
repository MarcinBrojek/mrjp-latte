.PHONY : all clean

all: latc_build latc_llvm_build

latc_llvm_build:
	cd src && \
	ghc -optc-malign-double -DLIBPATH='"$(shell realpath .)"' LLVMMain.hs -o ../latc_llvm

latc_llvm_tests: latc_llvm_bad_tests latc_llvm_good_tests latc_llvm_extensions_tests

latc_llvm_bad_tests:
	for test in lattests/bad/*.lat; do \
		echo $${test};\
		./latc_llvm $${test};\
		echo "\n";\
	done

latc_llvm_good_tests:
	for f in lattests/good/*.lat; do \
		echo "############################## $${f} #########################";\
		./latc_llvm $${f};\
		echo "---";\
		if [ -f "$${f%???}input" ]; then \
			lli "$${f%???}bc" < "$${f%???}input" > tmp.output; \
		else \
			lli "$${f%???}bc" > tmp.output; \
		fi; \
		if ! diff tmp.output "$${f%???}output" > diff.output; then \
			echo "DIFF"; \
			cat diff.output; \
		fi; \
	done

latc_llvm_extensions_tests:
	for f in lattests/extensions/*/*.lat; do \
		echo "############################## $${f} #########################";\
		./latc_llvm $${f};\
		echo "---";\
		if [ -f "$${f%???}input" ]; then \
			lli "$${f%???}bc" < "$${f%???}input" > tmp.output; \
		else \
			lli "$${f%???}bc" > tmp.output; \
		fi; \
		if ! diff tmp.output "$${f%???}output" > diff.output; then \
			echo "DIFF"; \
			cat diff.output; \
		fi; \
	done

latc_build: 
	cd src && \
	ghc -optc-malign-double TCMain.hs -o ../latc

latc_tests: latc_bad_tests latc_good_tests latc_extensions_tests

latc_bad_tests:
	for test in lattests/bad/*.lat; do \
		echo $${test};\
		./latc $${test};\
		echo "\n";\
	done

latc_good_tests:
	for test in lattests/good/*.lat; do \
		echo $${test};\
		./latc $${test};\
		echo "\n";\
	done

latc_extensions_tests:
	for test in lattests/extensions/*/*.lat; do \
		echo $${test};\
		./latc $${test};\
		echo "\n";\
	done

clean:
	rm -rf latc && \
	rm -rf latc_llvm && \
	rm diff.output && \
	rm tmp.output && \
	cd src && \
	rm -rf *.o *.hi