build:
	elm-app build

deploy: build
	now && now alias -A ./now.json
