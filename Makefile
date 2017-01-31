### Lunchbox main engine

current: target

target pngtarget pdftarget vtarget acrtarget: lmertest.Rout 

#################################################################

Sources += Makefile stuff.mk LICENSE.md
include stuff.mk
-include $(ms)/git.def

Sources += todo.md

redundancy.Rout: redundancy.R

lmtest.Rout: redundancy.Rout lmtest.R

lmertest.Rout: redundancy.Rout lmertest.R

clmmtest.Rout: redundancy.Rout clmmtest.R

#############

-include $(ms)/git.mk
-include $(ms)/visual.mk
-include $(ms)/linux.mk
-include $(ms)/wrapR.mk
-include rmd.mk
