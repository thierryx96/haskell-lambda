import { Response } from "node-fetch";
export interface MonitorParams {
    [key: string]: any;
}
export interface Monitor {
    [key: string]: MonitorParams;
}
/**
 * Adds the appropriate tags and required parameters that will be passed as part of the request body for creating and updating monitors
 * @param monitor - the Monitor object that is defined in the serverless.yml file
 * @param cloudFormationStackId - the Cloud Formation Stack ID
 * @param service - the Service
 * @param env  - the Environment
 * @returns valid monitor parameters
 */
export declare function buildMonitorParams(monitor: Monitor, cloudFormationStackId: string, service: string, env: string): {
    [x: string]: any;
};
/**
 * Handles the Monitor API response and logs the appropriate error
 * @param response Monitor API Response
 * @param serverlessMonitorId Serverless Monitor ID
 */
export declare function handleMonitorsApiResponse(response: Response, serverlessMonitorId?: string): boolean;
/**
 * Creates, updates, and deletes the appropriate monitor configurations as defined in the serverless.yml file
 * @param monitors - Monitors defined in the serverless.yml file
 * @param monitorsApiKey - the API Key
 * @param monitorsAppKey - the Application Key
 * @param cloudFormationStackId - the Cloud Formation Stack ID
 * @param service - the Service
 * @param env - the Environment
 * @returns monitors that have been successfully created, updated, and deleted according to the configuration defined in the plugin
 */
export declare function setMonitors(site: string, monitors: Monitor[], monitorsApiKey: string, monitorsAppKey: string, cloudFormationStackId: string, service: string, env: string): Promise<string[]>;
//# sourceMappingURL=monitors.d.ts.map