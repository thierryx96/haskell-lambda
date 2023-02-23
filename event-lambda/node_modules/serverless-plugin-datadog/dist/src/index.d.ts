import { FunctionDefinition } from "serverless";
export interface ExtendedFunctionDefinition extends FunctionDefinition {
    tags?: {
        [key: string]: string;
    };
    environment?: {
        [key: string]: string;
    };
}
//# sourceMappingURL=index.d.ts.map