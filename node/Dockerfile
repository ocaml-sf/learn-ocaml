
FROM node:12-alpine 

WORKDIR /node

COPY package*.json ./

RUN npm install && npm cache clean --force --loglevel=error

WORKDIR /node/app

COPY ./app.js app.js

CMD [ "node", "app.js"]
