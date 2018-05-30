FROM fpco/stack-build:latest
WORKDIR /app
COPY . /app
RUN cd /app && stack build
RUN stack exec perservant
