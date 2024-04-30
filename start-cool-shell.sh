MSYS_NO_PATHCONV=1 docker pull boylanduwm/cool-compiler
MSYS_NO_PATHCONV=1 docker run -v "`pwd`:/root" -it --rm -w /root boylanduwm/cool-compiler bash
