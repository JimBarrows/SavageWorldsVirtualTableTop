if [ -d "build" ]; then 
    rm -rf build
fi
mkdir build
echo "Writing manifest file"
sbcl --no-userinit --no-sysinit --non-interactive --load ~/quicklisp/setup.lisp --eval '(ql:write-asdf-manifest-file "build/quicklisp-manifest.txt")'

echo "Building server"
buildapp --output build/savage-worlds --manifest-file build/quicklisp-manifest.txt --manifest-file ~/quicklisp/local-projects/system-index.txt --asdf-tree . --load-system savage-worlds --entry savage-worlds-api::main
cd web
echo "building UI"
ember build --environment production
tar -czf ../build/savage-worlds-web.tar.gz dist/*
