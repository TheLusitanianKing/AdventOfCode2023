FROM ocaml/opam:debian-12-ocaml-4.08-fp

# install basic dependencies
RUN opam install dune.2.9.3 core.v0.12.4 batteries.3.7.1
# install cohttp dependencies
RUN sudo apt update && sudo apt install -y libssl-dev pkg-config libgmp-dev && \
  opam install cohttp-lwt-unix.5.2.0 cohttp-async.5.2.0 lwt_ssl
# install more dependencies
RUN opam install ppx_deriving yojson.2.1.0

# install IDE/dev specific dependencies
RUN opam install ocaml-lsp-server utop ocamlformat
