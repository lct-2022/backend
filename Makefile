all:
	# базовый слой для всех сервисов, с библиотеками-зависимостями
	docker build -t svetlyak40wt/lct-2022-base:latest -f docker/Dockerfile.base .
	# Сервис rating
	docker build -t svetlyak40wt/lct-2022-rating:latest -f docker/Dockerfile --build-arg APP=rating .
