all:
	# базовый слой для всех сервисов, с библиотеками-зависимостями
	docker build -t svetlyak40wt/lct-2022-base:latest -f docker/Dockerfile.base .
	# Сервисы
	docker build -t svetlyak40wt/lct-2022-rating:latest -f docker/Dockerfile --build-arg APP=rating .
	docker build -t svetlyak40wt/lct-2022-passport:latest -f docker/Dockerfile --build-arg APP=passport .
	docker build -t svetlyak40wt/lct-2022-platform:latest -f docker/Dockerfile --build-arg APP=platform .
	docker build -t svetlyak40wt/lct-2022-chat:latest -f docker/Dockerfile --build-arg APP=chat .
	docker build -t svetlyak40wt/lct-2022-app:latest -f docker/Dockerfile --build-arg APP=app .
