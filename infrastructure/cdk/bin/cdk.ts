#!/usr/bin/env node
import * as cdk from 'aws-cdk-lib';
import { NetworkStack } from '../lib/stacks/network-stack';
import { DatabaseStack } from '../lib/stacks/database-stack';
import { BackendStack } from '../lib/stacks/backend-stack';
import { FrontendStack } from '../lib/stacks/frontend-stack';
import { MonitoringStack } from '../lib/stacks/monitoring-stack-simple';
import * as fs from 'fs';
import * as path from 'path';

const app = new cdk.App();

// Get environment from context or default to 'dev'
const environment = app.node.tryGetContext('environment') || 'dev';

// Load configuration for the environment
const configPath = path.join(__dirname, '..', 'config', `${environment}.json`);
let config: any = {};

try {
  const configContent = fs.readFileSync(configPath, 'utf8');
  config = JSON.parse(configContent);
} catch (error) {
  console.warn(`Could not load configuration for environment '${environment}'. Using defaults.`);
  config = { environment, region: 'us-east-1' };
}

// Set up environment configuration
const env: cdk.Environment = {
  account: process.env.CDK_DEFAULT_ACCOUNT || config.accountId?.replace('${AWS_ACCOUNT_ID}', process.env.CDK_DEFAULT_ACCOUNT || ''),
  region: process.env.CDK_DEFAULT_REGION || config.region || 'us-east-1',
};

// Create Network Stack
const networkStack = new NetworkStack(app, `SWVTT-Network-${environment}`, {
  env,
  environment,
  vpcCidr: config.network?.vpcCidr,
  description: `Network infrastructure for SWVTT ${environment} environment`,
});

// Create Database Stack
const databaseStack = new DatabaseStack(app, `SWVTT-Database-${environment}`, {
  env,
  environment,
  vpc: networkStack.vpc,
  databaseName: 'swvtt',
  instanceClass: config.database?.instanceClass || 'db.t3.micro',
  description: `Database infrastructure for SWVTT ${environment} environment`,
});

// Create Backend Stack
const backendStack = new BackendStack(app, `SWVTT-Backend-${environment}`, {
  env,
  environment,
  vpc: networkStack.vpc,
  databaseSecret: databaseStack.databaseSecret,
  databaseEndpoint: databaseStack.database.instanceEndpoint.hostname,
  privateSubnets: networkStack.privateSubnets,
  publicSubnets: networkStack.publicSubnets,
  description: `Backend infrastructure for SWVTT ${environment} environment`,
});

// Create Frontend Stack
const frontendStack = new FrontendStack(app, `SWVTT-Frontend-${environment}`, {
  env,
  environment,
  backendUrl: backendStack.loadBalancer.loadBalancerDnsName,
  domainName: config.frontend?.domainName,
  description: `Frontend infrastructure for SWVTT ${environment} environment`,
});

// Create Monitoring Stack
const monitoringStack = new MonitoringStack(app, `SWVTT-Monitoring-${environment}`, {
  env,
  environment,
  alertEmail: config.monitoring?.alertEmail,
  description: `Monitoring infrastructure for SWVTT ${environment} environment`,
});

// Add stack dependencies
databaseStack.addDependency(networkStack);
backendStack.addDependency(networkStack);
backendStack.addDependency(databaseStack);
frontendStack.addDependency(backendStack);
monitoringStack.addDependency(networkStack);
monitoringStack.addDependency(databaseStack);
monitoringStack.addDependency(backendStack);

// Apply global tags from configuration
if (config.tags) {
  Object.entries(config.tags).forEach(([key, value]) => {
    cdk.Tags.of(app).add(key, value as string);
  });
}

// Add environment-specific tags
cdk.Tags.of(app).add('CDK-Environment', environment);
cdk.Tags.of(app).add('CDK-App', 'SWVTT');

app.synth();