import { FunctionInfo } from "./layer";
export declare const datadogHandlerEnvVar = "DD_LAMBDA_HANDLER";
export declare const pythonHandler = "datadog_lambda.handler.handler";
export declare const jsHandlerWithLayers = "/opt/nodejs/node_modules/datadog-lambda-js/handler.handler";
export declare const jsHandler = "node_modules/datadog-lambda-js/dist/handler.handler";
/**
 * For each lambda function, redirects handler to the Datadog handler for the given runtime,
 * and sets Datadog environment variable `DD_LAMBDA_HANDLER` to the original handler.
 */
export declare function redirectHandlers(funcs: FunctionInfo[], addLayers: boolean, customHandler?: string): void;
//# sourceMappingURL=wrapper.d.ts.map