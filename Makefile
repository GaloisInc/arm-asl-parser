.PHONY: default clean realclean
default: all

VERSION = 00bet9
XMLDIR = xml
ASLDIR = asl
PARSEDIR = asl-parsed

MRA_TOOLS := $(abspath ./submodules/mra_tools)
ASL_PARSER = asl-parser-java


SYSREG_TAR=${XMLDIR}/ARMv85A-SysReg-${VERSION}.tar.gz
A64_TAR=${XMLDIR}/A64_v85A_ISA_xml_${VERSION}.tar.gz
A32_TAR=${XMLDIR}/AArch32_v85A_ISA_xml_${VERSION}.tar.gz

TARFILES = ${SYSREG_TAR} ${A64_TAR} ${A32_TAR}

A64 = ${XMLDIR}/ISA_v85A_A64_xml_${VERSION}
A32 = ${XMLDIR}/ISA_v85A_AArch32_xml_${VERSION}
SYSREG = ${XMLDIR}/SysReg_v85A_xml-${VERSION}

${XMLDIR}:
	mkdir -p ${XMLDIR}

${ASLDIR}:
	mkdir -p ${ASLDIR}

${PARSEDIR}:
	mkdir -p ${PARSEDIR}

${XMLDIR}/%.tar.gz: | ${XMLDIR}
	cd ${XMLDIR} && \
	wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/$(@F)

define ASLTARGET
$($T): $($T_TAR)
	cd ${XMLDIR} && \
	tar zxf $$(<F) && tar zxf $$(@F).tar.gz
endef

ASLTARGETS=A64 A32
$(foreach T,$(ASLTARGETS), $(eval $(ASLTARGET)))

${SYSREG}: ${SYSREG_TAR}
	cd ${XMLDIR} && \
	tar zxf $(<F)

${ASLDIR}/arch.asl ${ASLDIR}/arch_instrs.asl: ${A64} ${A32} | ${ASLDIR}
	cd ${ASLDIR} && \
	${MRA_TOOLS}/bin/instrs2asl.py --demangle --verbose $(abspath $^) && \
	sed -i -e 's/type1 PARTIDtype/type PARTIDtype/;s/type1 PMGtype/type PMGtype/;s/type1 MPAMinfo/type MPAMinfo/' $(abspath $@)

${ASLDIR}/support.asl:
	cat ${MRA_TOOLS}/support/*.asl > $@

${ASLDIR}/regs.asl: ${SYSREG}
	${MRA_TOOLS}/bin/reg2asl.py $< -o $@

define PARSETARGET
${PARSEDIR}/$(1).sexpr: ${ASLDIR}/$(2).asl | ${PARSEDIR}
	cd ${ASL_PARSER} && \
	./gradlew -q run --args="$(3) $$(abspath $$<)" > $$(abspath $$@) && \
	[[ -s $$(abspath $$@) ]] || (rm -f $$(abspath $$@) && exit 1)

ASL_PARSED += ${PARSEDIR}/$(1).sexpr
endef

$(eval $(call PARSETARGET,arm_defs,arch,defs))
$(eval $(call PARSETARGET,arm_instrs,arch_instrs,inst))
$(eval $(call PARSETARGET,arm_regs,regs,regs))
$(eval $(call PARSETARGET,support,support,defs))

all: ${ASL_PARSED}

clean:
	rm -rf ${ASLDIR} ${PARSEDIR}

realclean: clean
	rm -rf ${XMLDIR}
