#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2010-2012. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#

ERLC=erlc
ERLCINCLUDES=-pa ./dict
ERLCFLAGS=-Wall +debug_info

all:
	@ $(MAKE) -C dict
	@ $(ERLC) $(ERLCINCLUDES) $(ERLCFLAGS) *.erl ;

clean:
	@ rm -rf *.beam erl_crash.dump
	@ $(MAKE) -C dict clean

.PHONY: all clean
