FROM node:lts-alpine as FRONTEND_BUILD

WORKDIR /frontend
COPY ./front /frontend

RUN npm install
RUN node build.js

FROM alpine:latest as APPLICATION

RUN apk add postgresql-client sbcl

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
 
CMD sbcl --load "usufslc.asd" \
         --eval "(ql:quickload 'usufslc)" \
         --eval "(usufslc:start)" \
         --eval "(bt:join-thread (find-if (lambda (th)(search \"hunchentoot\" (bt:thread-name th)))(bt:all-threads)))"
