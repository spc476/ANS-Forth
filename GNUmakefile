#**********************************************************************
#   Makefile for ANS Forth 2012 for the 6809 CPU
#   Copyright (C) 2025 by Sean Conner.
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY# without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#   Comments, questions and criticisms can be sent to: sean@conman.org
#
#**********************************************************************

A09      = a09
A09FLAGS = -T

.PHONY: clean

%.bin : %.asm
	$(A09) $(A09FLAGS) -o $@ $<

forth.bin :
clean:
	$(RM) *~ *.bin
