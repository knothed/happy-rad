ENV = .local.env

EXECUTABLE = "happy-rad"
PACKAGES = ["happy-rad", "rad-backend", "happy-core", "happy-frontend", "happy-middleend", "happy-backend", "happy-test"]
BOOTSTRAPPING = False

sdist-test ::
	rm -f ${ENV}
	cabal v2-install --lib happy-test --package-env ${ENV}
	ghc -package-env ${ENV} -e 'SDist.sdist_test "$(shell pwd)" ${EXECUTABLE} ${PACKAGES} ${BOOTSTRAPPING}'
	rm -f ${ENV}