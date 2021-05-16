FROM clojure:lein-alpine

RUN mkdir /app
WORKDIR /app
COPY project.clj /app/project.clj
RUN lein deps
COPY . /app
RUN lein bin

CMD ["bin/slackat"]
