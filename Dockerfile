FROM ubuntu:18.04

RUN apt-get update && \
			apt-get install -y mit-scheme

WORKDIR /app

COPY . ./

ENTRYPOINT ["scheme", "--silent", "--load"]
