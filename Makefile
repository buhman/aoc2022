INPUT += input/day1
INPUT += input/day2
INPUT += input/day3
INPUT += input/day4
INPUT += input/day5

all: $(INPUT)

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
