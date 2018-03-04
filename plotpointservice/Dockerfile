FROM node:9.5

EXPOSE 80

RUN mkdir -p /usr/src/app/node_modules
WORKDIR /usr/src/app

COPY ./ .
RUN npm install --global nodemon babel-core babel-cli
RUN npm install --production

CMD [ "nodemon", "index.js"]
