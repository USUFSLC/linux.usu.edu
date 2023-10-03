FROM node:18 as FRONTEND_BUILD

WORKDIR /frontend
COPY ./front /frontend

RUN npm install
RUN node build.js

FROM debian:bookworm as APPLICATION

RUN apt-get -qq update
RUN apt-get -y install gcc postgresql-client sbcl libev-dev

WORKDIR /app
COPY . /app

COPY --from=FRONTEND_BUILD /frontend/dist ./front/dist

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp

RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --quit && \
  echo '#-quicklisp (load #P"/root/quicklisp/setup.lisp")' > /root/.sbclrc && \
  rm /root/quicklisp.lisp
 
CMD sbcl --load run.lisp
