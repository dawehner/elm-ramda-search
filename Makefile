build:
	elm-app build

deploy: build
	now
