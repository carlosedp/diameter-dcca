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

APPS  = client server
SUPPORT = common_stats ocs_intm
CALLBACKS = $(APPS:%=%_cb)

MODULES   = $(APPS) $(APPS:%=%_cb) $(SUPPORT)

BEAM = $(MODULES:%=%.beam)

DICTS = rfc4006_cc_Gy

all: $(BEAM)

%.beam: %.erl dicts
	erlc -Wall +debug_info $<

dicts:
	$(MAKE) -C dict
	cp dict/$(DICTS).hrl dict/$(DICTS).beam dict/$(DICTS).erl .

clean:
	rm -f $(BEAM) $(DICTS).beam
	$(MAKE) -C dict clean

.PHONY: all clean
