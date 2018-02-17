# makefile for haskell players by Shuji Kinoshita since 2017-01-07

HASKFLAGS= -Wall -O2 
HASK= ghc

# TARGET= hogePlayer piyoPlayer
#TARGET= safePlayer
TARGET= safePlayer2

all: ${TARGET}

# This would be changed. (compile all players at once?)
hogePlayer: hogePlayer.hs Initialization.hs PerStep.hs Information.hs
	${HASK} ${HASKFLAGS} $^

safePlayer: safePlayer.hs Initialization.hs PerStep.hs Information.hs
	${HASK} ${HASKFLAGS} $^

safePlayer2: safePlayer2.hs Initialization.hs PerStep.hs Information.hs
	${HASK} ${HASKFLAGS} $^

# piyoPlayer: piyoPlayer.hs
#	${HASK} ${HASKFLAGS} $^
#	mv $@ ../

clean:
	rm -f ${TARGET}
	rm -f *.o *.hi
