.PHONY: deploy

GIT_BRANCH=`git rev-parse --symbolic-full-name --abbrev-ref HEAD`
ENVIRONMENT=github-pages

deploy:
	curl -X POST -H "Authorization: token ${GITHUB_TOKEN}" \
		-H "Accept: application/vnd.github.ant-man-preview+json"  \
		-H "Content-Type: application/json" \
		https://api.github.com/repos/PaackEng/paack-ui/deployments \
		--data '{"ref": "'${GIT_BRANCH}'", "environment": "${ENVIRONMENT}", "required_contexts": [], "auto_merge": false}'
