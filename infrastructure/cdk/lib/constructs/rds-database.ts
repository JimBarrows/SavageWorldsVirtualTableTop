import * as cdk from 'aws-cdk-lib';
import * as rds from 'aws-cdk-lib/aws-rds';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch';
import * as sns from 'aws-cdk-lib/aws-sns';
import { Construct } from 'constructs';

export interface RdsDatabaseProps {
  vpc: ec2.IVpc;
  environment: string;
  databaseName: string;
  engine: rds.IInstanceEngine;
  instanceClass: ec2.InstanceType;
  allocatedStorage?: number;
  maxAllocatedStorage?: number;
  backupRetentionDays?: number;
  multiAz?: boolean;
  enablePerformanceInsights?: boolean;
  enableCloudwatchLogsExports?: string[];
  deletionProtection?: boolean;
  allowedSecurityGroups?: ec2.ISecurityGroup[];
  allowedCidr?: string[];
  parameterGroupName?: string;
  monitoringInterval?: number;
}

export class RdsDatabase extends Construct {
  public readonly instance: rds.DatabaseInstance;
  public readonly secret: secretsmanager.Secret;
  public readonly securityGroup: ec2.SecurityGroup;
  public readonly subnetGroup: rds.SubnetGroup;
  public readonly parameterGroup?: rds.ParameterGroup;

  constructor(scope: Construct, id: string, props: RdsDatabaseProps) {
    super(scope, id);

    // Create database secret
    this.secret = new secretsmanager.Secret(this, 'DatabaseSecret', {
      secretName: `${props.databaseName}/${props.environment}/credentials`,
      description: `Database credentials for ${props.databaseName} ${props.environment}`,
      generateSecretString: {
        secretStringTemplate: JSON.stringify({ 
          username: `${props.databaseName}admin`.toLowerCase(),
        }),
        generateStringKey: 'password',
        excludeCharacters: '"@/\\\'',
        includeSpace: false,
        passwordLength: 32,
      },
    });

    // Create subnet group for database
    this.subnetGroup = new rds.SubnetGroup(this, 'SubnetGroup', {
      description: `Subnet group for ${props.databaseName} ${props.environment} database`,
      vpc: props.vpc,
      subnetGroupName: `${props.databaseName}-${props.environment}-subnet-group`,
      vpcSubnets: {
        subnetType: ec2.SubnetType.PRIVATE_ISOLATED,
      },
    });

    // Create security group for database
    this.securityGroup = new ec2.SecurityGroup(this, 'DatabaseSecurityGroup', {
      vpc: props.vpc,
      description: `Security group for ${props.databaseName} ${props.environment} database`,
      allowAllOutbound: false,
    });

    // Determine database port based on engine
    const port = this.getDatabasePort(props.engine);

    // Allow connections from specified security groups
    if (props.allowedSecurityGroups) {
      props.allowedSecurityGroups.forEach((sg, index) => {
        this.securityGroup.addIngressRule(
          ec2.Peer.securityGroupId(sg.securityGroupId),
          ec2.Port.tcp(port),
          `Allow database connections from security group ${index + 1}`
        );
      });
    }

    // Allow connections from specified CIDR blocks
    if (props.allowedCidr) {
      props.allowedCidr.forEach((cidr, index) => {
        this.securityGroup.addIngressRule(
          ec2.Peer.ipv4(cidr),
          ec2.Port.tcp(port),
          `Allow database connections from CIDR ${cidr}`
        );
      });
    }

    // If no specific access is defined, allow from VPC CIDR
    if (!props.allowedSecurityGroups && !props.allowedCidr) {
      // Use a default CIDR for VPC access
      this.securityGroup.addIngressRule(
        ec2.Peer.ipv4('10.0.0.0/16'),
        ec2.Port.tcp(port),
        'Allow database connections from VPC'
      );
    }

    // Create parameter group if custom parameters are needed
    if (props.parameterGroupName) {
      this.parameterGroup = new rds.ParameterGroup(this, 'ParameterGroup', {
        engine: props.engine,
        description: `Parameter group for ${props.databaseName} ${props.environment}`,
        parameters: this.getDefaultParameters(props.engine),
      });
    }

    // Create RDS instance
    this.instance = new rds.DatabaseInstance(this, 'DatabaseInstance', {
      engine: props.engine,
      instanceType: props.instanceClass,
      instanceIdentifier: `${props.databaseName}-${props.environment}`,
      vpc: props.vpc,
      subnetGroup: this.subnetGroup,
      securityGroups: [this.securityGroup],
      credentials: rds.Credentials.fromSecret(this.secret),
      databaseName: props.databaseName.toLowerCase(),
      
      // Storage configuration
      allocatedStorage: props.allocatedStorage || (props.environment === 'prod' ? 100 : 20),
      maxAllocatedStorage: props.maxAllocatedStorage || (props.environment === 'prod' ? 1000 : 100),
      storageType: rds.StorageType.GP2,
      storageEncrypted: true,
      
      // Backup and maintenance
      backupRetention: cdk.Duration.days(
        props.backupRetentionDays || (props.environment === 'prod' ? 30 : 7)
      ),
      deleteAutomatedBackups: props.environment !== 'prod',
      deletionProtection: props.deletionProtection ?? (props.environment === 'prod'),
      
      // High availability
      multiAz: props.multiAz ?? (props.environment === 'prod'),
      
      // Performance monitoring
      enablePerformanceInsights: props.enablePerformanceInsights ?? (props.environment === 'prod'),
      performanceInsightRetention: props.enablePerformanceInsights 
        ? rds.PerformanceInsightRetention.MONTHS_3 
        : undefined,
      
      // Monitoring interval in seconds - use Duration
      monitoringInterval: cdk.Duration.seconds(props.monitoringInterval || (props.environment === 'prod' ? 60 : 0)),
      
      // Logging
      cloudwatchLogsExports: props.enableCloudwatchLogsExports || [],
      
      // Maintenance
      autoMinorVersionUpgrade: true,
      allowMajorVersionUpgrade: false,
      
      // Parameter group
      parameterGroup: this.parameterGroup,
      
      removalPolicy: props.environment === 'prod' 
        ? cdk.RemovalPolicy.SNAPSHOT 
        : cdk.RemovalPolicy.DESTROY,
    });

    // Add tags
    cdk.Tags.of(this).add('Database', props.databaseName);
    cdk.Tags.of(this).add('Environment', props.environment);
  }

  /**
   * Get the appropriate port for the database engine
   */
  private getDatabasePort(engine: rds.IInstanceEngine): number {
    const engineType = engine.engineType;
    
    switch (engineType) {
      case 'mysql':
        return 3306;
      case 'postgres':
        return 5432;
      case 'oracle-ee':
      case 'oracle-se2':
      case 'oracle-se1':
      case 'oracle-se':
        return 1521;
      case 'sqlserver-ee':
      case 'sqlserver-se':
      case 'sqlserver-ex':
      case 'sqlserver-web':
        return 1433;
      default:
        return 3306; // Default to MySQL port
    }
  }

  /**
   * Get default parameters based on engine type and environment
   */
  private getDefaultParameters(engine: rds.IInstanceEngine): { [key: string]: string } {
    const engineType = engine.engineType;
    const baseParameters: { [key: string]: string } = {};
    
    if (engineType === 'mysql') {
      baseParameters['innodb_buffer_pool_size'] = '{DBInstanceClassMemory*3/4}';
      baseParameters['max_connections'] = '200';
      baseParameters['slow_query_log'] = '1';
      baseParameters['long_query_time'] = '2';
      baseParameters['log_queries_not_using_indexes'] = '1';
    } else if (engineType === 'postgres') {
      baseParameters['shared_preload_libraries'] = 'pg_stat_statements';
      baseParameters['log_statement'] = 'all';
      baseParameters['log_min_duration_statement'] = '1000';
      baseParameters['max_connections'] = '200';
    }
    
    return baseParameters;
  }

  /**
   * Create CloudWatch alarms for database monitoring
   */
  public createCloudWatchAlarms(alarmTopic?: cdk.aws_sns.ITopic): cloudwatch.Alarm[] {
    const alarms: cloudwatch.Alarm[] = [];

    // High CPU utilization alarm
    const cpuAlarm = new cloudwatch.Alarm(this, 'HighCPUAlarm', {
      alarmName: `${this.instance.instanceIdentifier}-high-cpu`,
      alarmDescription: 'Database CPU utilization is too high',
      metric: this.instance.metricCPUUtilization(),
      threshold: 80,
      evaluationPeriods: 3,
      datapointsToAlarm: 2,
      treatMissingData: cloudwatch.TreatMissingData.NOT_BREACHING,
    });
    alarms.push(cpuAlarm);

    // High connection count alarm
    const connectionAlarm = new cloudwatch.Alarm(this, 'HighConnectionsAlarm', {
      alarmName: `${this.instance.instanceIdentifier}-high-connections`,
      alarmDescription: 'Database connection count is too high',
      metric: this.instance.metricDatabaseConnections(),
      threshold: 80,
      evaluationPeriods: 2,
      treatMissingData: cloudwatch.TreatMissingData.NOT_BREACHING,
    });
    alarms.push(connectionAlarm);

    // Low free storage alarm
    const storageAlarm = new cloudwatch.Alarm(this, 'LowStorageAlarm', {
      alarmName: `${this.instance.instanceIdentifier}-low-storage`,
      alarmDescription: 'Database free storage space is too low',
      metric: new cloudwatch.Metric({
        namespace: 'AWS/RDS',
        metricName: 'FreeStorageSpace',
        dimensionsMap: {
          DBInstanceIdentifier: this.instance.instanceIdentifier,
        },
      }),
      threshold: 2000000000, // 2GB in bytes
      comparisonOperator: cloudwatch.ComparisonOperator.LESS_THAN_THRESHOLD,
      evaluationPeriods: 2,
      treatMissingData: cloudwatch.TreatMissingData.NOT_BREACHING,
    });
    alarms.push(storageAlarm);

    // Add alarm actions if SNS topic is provided
    if (alarmTopic) {
      alarms.forEach(alarm => {
        alarm.addAlarmAction({
          bind: () => ({ alarmActionArn: alarmTopic.topicArn }),
        });
      });
    }

    return alarms;
  }

  /**
   * Grant read access to the database secret
   */
  public grantSecretRead(grantee: cdk.aws_iam.IGrantable): cdk.aws_iam.Grant {
    return this.secret.grantRead(grantee);
  }

  /**
   * Get the database endpoint
   */
  public getEndpoint(): string {
    return this.instance.instanceEndpoint.hostname;
  }

  /**
   * Get the database port
   */
  public getPort(): number {
    return this.instance.instanceEndpoint.port;
  }

  /**
   * Get connection string format
   */
  public getConnectionStringTemplate(): string {
    const engine = this.instance.engine?.engineType;
    const endpoint = this.getEndpoint();
    const port = this.getPort();
    
    switch (engine) {
      case 'mysql':
        return `mysql://{username}:{password}@${endpoint}:${port}/{database}`;
      case 'postgres':
        return `postgresql://{username}:{password}@${endpoint}:${port}/{database}`;
      default:
        return `{engine}://{username}:{password}@${endpoint}:${port}/{database}`;
    }
  }
}