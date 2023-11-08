FROM node:18 as FRONTEND_BUILD

WORKDIR /frontend
COPY ./front /frontend

RUN npm install
RUN node build.js

FROM alfg/nginx-rtmp:cuda as RTMP-FRONTEND-SRV
COPY ./nginx/nginx.conf /etc/nginx/nginx.conf.template

COPY --from=FRONTEND_BUILD /frontend/dist ./front/dist

CMD /opt/entrypoint.sh
