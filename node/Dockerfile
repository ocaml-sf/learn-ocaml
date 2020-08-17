
FROM node:12-alpine 

WORKDIR /node

COPY package*.json ./

RUN npm install && npm cache clean --force --loglevel=error

WORKDIR /node/app

COPY ./app.js app.js
EXPOSE 8000
CMD [ "node", "app.js"]


