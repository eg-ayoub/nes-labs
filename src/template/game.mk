GAME := template
MAPPER := nrom

.PHONY: $(GAME)
$(GAME): $(BUILDDIR)/$(GAME)/rom.nes
	@echo "Done building $@"

$(BUILDDIR)/$(GAME)/rom.nes: $(BUILDDIR)/$(GAME)/rom.o
	$(LD) $^ $(LDFLAGS) -C $(MAPPER).cfg -o $@

$(BUILDDIR)/$(GAME)/rom.o: src/$(GAME)/cartridge.s
	mkdir -p $(BUILDDIR)/$(GAME)
	$(CA) $< $(CAFLAGS) -o $@

.PHONY: $(GAME)-clean
$(GAME)-clean:
	rm -rf $(BUILDDIR)/$(GAME)
	@echo "Done cleaning $@"



