FROM fukamachi/qlot:latest AS BUILDER

WORKDIR /app
COPY . /app

RUN qlot install
RUN qlot bundle

FROM alpine:3 as APPLICATION

RUN apk update
RUN apk add libev build-base postgresql15-client sbcl

WORKDIR /app
COPY --from=BUILDER /app /app

RUN sbcl --load .bundle-libs/bundle.lisp --load usufslc.asd --eval "(asdf:load-system :usufslc)"
CMD sbcl --load .bundle-libs/bundle.lisp --load run.lisp
