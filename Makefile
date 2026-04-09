# libtess2 Pascal port Makefile

# Include Makefile.node if it exists (author's environment)
-include Makefile.node

PROJNAME:=fpc-libtess2

# Test targets
test:
	$(MAKE) -C test/simple test

test-win64:
	$(MAKE) -C test/simple test-win64

test-clean:
	$(MAKE) -C test/simple clean

ifneq ($(wildcard Makefile.node),)
export:
	$(MKDIRDEEP) $(OPENSOURCEDIR)/$(PROJNAME)/.github/workflows
	$(MKDIRDEEP) $(OPENSOURCEDIR)/$(PROJNAME)/test/simple
	$(CP) $(CURDIR)/README.md $(OPENSOURCEDIR)/$(PROJNAME)/
	$(CP) $(CURDIR)/LICENSE $(OPENSOURCEDIR)/$(PROJNAME)/
	$(CP) $(CURDIR)/Makefile $(OPENSOURCEDIR)/$(PROJNAME)/Makefile
	$(CP) $(CURDIR)/examples/simple/simple.pas $(OPENSOURCEDIR)/$(PROJNAME)/examples/simple/simple.pas
	$(CP) $(CURDIR)/examples/simple/Makefile $(OPENSOURCEDIR)/$(PROJNAME)/examples/simple/Makefile
	$(CP) $(CURDIR)/libtess2_tesselator.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_tesselator.pas
	$(CP) $(CURDIR)/libtess2_bucketalloc.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_bucketalloc.pas
	$(CP) $(CURDIR)/libtess2_dict.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_dict.pas
	$(CP) $(CURDIR)/libtess2_geom.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_geom.pas
	$(CP) $(CURDIR)/libtess2_mesh.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_mesh.pas
	$(CP) $(CURDIR)/libtess2_priorityq.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_priorityq.pas
	$(CP) $(CURDIR)/libtess2_structs.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_structs.pas
	$(CP) $(CURDIR)/libtess2_sweep.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_sweep.pas
	$(CP) $(CURDIR)/libtess2_tesselator.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_tesselator.pas
	$(CP) $(CURDIR)/libtess2_trite.pas $(OPENSOURCEDIR)/$(PROJNAME)/src/libtess2_trite.pas
	$(CP) $(CURDIR)/.github/workflows/test.yml $(OPENSOURCEDIR)/$(PROJNAME)/.github/workflows/test.yml
	$(CP) $(CURDIR)/test/simple/Makefile $(OPENSOURCEDIR)/$(PROJNAME)/test/simple/Makefile
	$(CP) $(CURDIR)/test/simple/*.pas $(OPENSOURCEDIR)/$(PROJNAME)/test/simple/
endif
