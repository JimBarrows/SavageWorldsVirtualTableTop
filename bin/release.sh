cd bin
sbcl --load build.lisp --eval "(build:build)" --quit
cd ../web
ember build --environment production
tar -czf ../build/savage-worlds-web.tar.gz dist/*
