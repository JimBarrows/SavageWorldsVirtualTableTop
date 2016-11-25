FROM node:7.2.0
#"start" : "docker run --rm --name decks --link mongo:mongo -v \"$PWD\":/usr/src/app -w /usr/src/app -p 3000:3000 node:6 nodemon app.js --exec babel-node",
ENV ENV=dev
EXPOSE 3000
VOLUME /usr/src/app
WORKDIR /usr/src/app
RUN npm install --global nodemon babel-cli
CMD nodemon --exec babel-node --exec babel-node --presets es2015,stage-2 ./bin/www


