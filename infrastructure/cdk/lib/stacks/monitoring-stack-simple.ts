import * as cdk from 'aws-cdk-lib';
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch';
import * as sns from 'aws-cdk-lib/aws-sns';
import * as snsSubscriptions from 'aws-cdk-lib/aws-sns-subscriptions';
import * as logs from 'aws-cdk-lib/aws-logs';
import { Construct } from 'constructs';

export interface MonitoringStackProps extends cdk.StackProps {
  environment: string;
  alertEmail?: string;
}

export class MonitoringStack extends cdk.Stack {
  public readonly dashboard: cloudwatch.Dashboard;
  public readonly alarmTopic: sns.Topic;

  constructor(scope: Construct, id: string, props: MonitoringStackProps) {
    super(scope, id, props);

    // Create SNS topic for alerts
    this.alarmTopic = new sns.Topic(this, 'SWVTTAlarmTopic', {
      topicName: `swvtt-${props.environment}-alarms`,
      displayName: `SWVTT ${props.environment} Alarms`,
    });

    // Add email subscription if provided
    if (props.alertEmail) {
      this.alarmTopic.addSubscription(
        new snsSubscriptions.EmailSubscription(props.alertEmail)
      );
    }

    // Create CloudWatch Dashboard
    this.dashboard = new cloudwatch.Dashboard(this, 'SWVTTDashboard', {
      dashboardName: `SWVTT-${props.environment}`,
    });

    // Add basic widgets
    this.dashboard.addWidgets(
      new cloudwatch.TextWidget({
        markdown: `# SWVTT ${props.environment.toUpperCase()} Environment Dashboard\n\nThis dashboard provides monitoring for the SWVTT application infrastructure.`,
        width: 24,
        height: 3,
      })
    );

    // Tag resources appropriately
    cdk.Tags.of(this).add('Project', 'SWVTT');
    cdk.Tags.of(this).add('Environment', props.environment);
    cdk.Tags.of(this).add('Stack', 'Monitoring');

    // Export monitoring resources
    new cdk.CfnOutput(this, 'DashboardURL', {
      value: `https://console.aws.amazon.com/cloudwatch/home?region=${cdk.Aws.REGION}#dashboards:name=${this.dashboard.dashboardName}`,
      exportName: `${props.environment}-swvtt-dashboard-url`,
      description: 'CloudWatch dashboard URL for SWVTT monitoring',
    });

    new cdk.CfnOutput(this, 'AlarmTopicArn', {
      value: this.alarmTopic.topicArn,
      exportName: `${props.environment}-swvtt-alarm-topic-arn`,
      description: 'SNS topic ARN for SWVTT alarms',
    });
  }
}