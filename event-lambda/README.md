# event-lambda

## Adding email template

aws ses delete-template --region ap-southeast-2 --template-name PlanManagerVerification 
aws ses create-template --region ap-southeast-2 --cli-input-json file://data/plan-manager-verification-email.json

## Run locally

serverless invoke local -f verificationRequestHandler --path /home/th/dev/kynd-platform/backend/apps/event-lambda/__tests__/event_validate_police.json


