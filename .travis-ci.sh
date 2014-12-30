case "$OCAML_VERSION,$OPAM_VERSION" in
3.12.1,1.0.0) ppa=avsm/ocaml312+opam10 ;;
3.12.1,1.1.0) ppa=avsm/ocaml312+opam11 ;;
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12 ;;
4.02.0,1.2.0) ppa=avsm/ocaml42+opam12 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
echo Updating package index
sudo apt-get update -qq
echo Installing OCaml
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

echo Installing opam
opam init -y
eval `opam config env`
opam pin add test-camldm . -y || true
sudo apt-get install $(opam install test-camldm -e ubuntu) -y
opam install test-camldm -y
make
# the tests need loop and dm drivers not available on travis
#make test
