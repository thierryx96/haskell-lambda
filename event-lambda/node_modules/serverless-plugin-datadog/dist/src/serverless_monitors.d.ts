export interface ServerlessMonitor {
    name: string;
    threshold: number;
    query: (cloudFormationStackId: string, criticalThreshold: number) => string;
    message: string;
    type?: string;
}
export declare const HIGH_ERROR_RATE: ServerlessMonitor;
export declare const TIMEOUT: ServerlessMonitor;
export declare const OUT_OF_MEMORY: ServerlessMonitor;
export declare const HIGH_ITERATOR_AGE: ServerlessMonitor;
export declare const HIGH_COLD_START_RATE: ServerlessMonitor;
export declare const HIGH_THROTTLES: ServerlessMonitor;
export declare const INCREASED_COST: ServerlessMonitor;
export declare const SERVERLESS_MONITORS: {
    [key: string]: ServerlessMonitor;
};
//# sourceMappingURL=serverless_monitors.d.ts.map