INPUT = $(addprefix input/day, $(shell seq 1 18))
PROMPT = $(addsuffix .html, $(addprefix prompt/day, $(shell seq 1 18)))

all: $(INPUT) #$(PROMPT)

input/day%: input.sh
	@mkdir -p $(dir $@)
	@./input.sh ${*} > $@.temp
	@mv $@.temp $@
	@echo get $@

prompt/day%.html: prompt.sh
	@mkdir -p $(dir $@)
	@./prompt.sh ${*} > $@.temp
	@mv $@.temp $@
	@echo get $@

clean:
	rm -f input/*
