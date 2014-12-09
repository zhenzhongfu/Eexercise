#---------------------------------start-----------------------------------------------------------------
VERSION=0.1.0 #首先定义版本

.PHONY: all compile deps clean eunit

# 默认为获取依赖OTP项目，然后进行编译
all: clean deps compile

help:
	@echo
	@echo "Usage: "
	@echo "       ./make {compile|clean|eunit}"
	@echo
	@echo
	@echo

#编译相关项目，在编译之前先查看依赖项目是否已经存在
compile: deps
	./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean

eunit:
	./rebar -C test.config eunit

#-------------------------end-------------------------------------------------------------------------
