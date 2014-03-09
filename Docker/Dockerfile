FROM walm/nginx

RUN apt-get update --fix-missing
RUN apt-get install -y curl git build-essential tmux

RUN apt-get -y install python-software-properties software-properties-common pkg-config m4 libssl-dev libgdbm-dev libsqlite3-dev libpcre3-dev camlp4-extra
RUN add-apt-repository -y ppa:avsm/ppa
RUN apt-get update
RUN apt-get -y --force-yes install ocaml opam

RUN opam init -y
RUN opam switch 4.01.0
RUN opam repository add ocsigendev https://github.com/ocsigen/opam-ocsigen.git

RUN opam config env
RUN opam update
RUN opam upgrade -y
RUN opam install -y menhir

RUN opam install -y ssl batteries lwt sqlite3-ocaml ocsigenserver eliom yojson cohttp bitcoin
RUN echo ". /.opam/opam-init/init.sh > /dev/null 2> /dev/null || true\neval `opam config env`" >> ~/.bashrc

RUN echo "#!/bin/bash\n\n. /.opam/opam-init/init.sh > /dev/null 2> /dev/null || true\neval `opam config env`\n" > /launch
RUN chmod 755 /launch

ENTRYPOINT ["/launch"]