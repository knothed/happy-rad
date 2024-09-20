# NOTE: This assumes that a working `ghc` is on $PATH; this may not
# necessarily be the same GHC used by `cabal` for building `happy`.
#
# Again, if HC has been set in the environment (e.g. by the CI), we keep this setting.
# [2021-07-14, PR #196](https://github.com/haskell/happy/pull/196)
#
HC ?= ghc
HC_OPTS=-package array -Wall -Werror

# NOTE: `cabal test` will take care to build the local `happy`
# executable and place it into $PATH for us to pick up.
# (This is ensured by setting build-tool-depends.)
#
# If it doesn't look like the alex binary in $PATH comes from the
# build tree, then we'll fall back to pointing to
# ../dist/build/alex/alex to support running tests via "runghc
# Setup.hs test".
#
# If HAPPY has been set outside, e.g. in the environment, we trust this setting.
# This way, we can pass in the correct Happy executable from a CI environment
# without danger of it being "fixed" by the logic below.
# [2021-07-14, PR #196](https://github.com/haskell/happy/pull/196)
#
HAPPY ?= happy-rad

.PRECIOUS: %.n.hs %.o %.exe %.bin

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
HS_PROG_EXT = .exe
else
HS_PROG_EXT = .bin
endif

TESTS = Test.ly TestMulti.ly TestPrecedence.ly bug001.ly \
	monad002.ly precedence001.ly precedence002.y \
	bogus-token.y bug002.y Partial.ly \
	issue91.y issue94.y \
	shift01.y \
        EpsilonAnnounce.y

TEST_HAPPY_OPTS = --types

%.n.hs : %.y
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

%.n.hs : %.ly
	$(HAPPY) $(TEST_HAPPY_OPTS) $< -o $@

CLEAN_FILES += *.n.hs *.info *.hi *.bin *.exe *.o *.run.stdout *.run.stderr

ALL_TEST_HS = $(shell echo $(TESTS) | sed -e 's/\([^\. ]*\)\.\(l\)\{0,1\}y/\1.n.hs /g')

ALL_TESTS = $(patsubst %.hs, %.run, $(ALL_TEST_HS))

HC_OPTS += -fforce-recomp

.PRECIOUS: %.hs %.o %.bin %.$(HS_PROG_EXT)

%.run : %$(HS_PROG_EXT)
	@echo "--> Checking $<..."
	./$<

path.run : # simply a test to output the path of the built happy executable, useful in CI
	@echo "--> Printing happy path..."
	which $(HAPPY)

%$(HS_PROG_EXT) : %.hs
	$(HC) $(HC_OPTS) $($*_LD_OPTS) $< -o $@

all :: path.run $(ALL_TESTS)

.PHONY: clean all path.run

clean:
	$(RM) $(CLEAN_FILES)
