fakall.boo.gz: fakall.boo
	PATH=.:$(PATH) zopfli --i1 fakall.boo

fakall.boo: *.xml
	cd ..; sbt "project modsParser" run "project indexer" run
