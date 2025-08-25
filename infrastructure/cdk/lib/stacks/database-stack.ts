import * as cdk from 'aws-cdk-lib';
import * as rds from 'aws-cdk-lib/aws-rds';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import { Construct } from 'constructs';

export interface DatabaseStackProps extends cdk.StackProps {
  environment: string;
  vpc: ec2.IVpc;
  databaseName?: string;
  instanceClass?: string;
}

export class DatabaseStack extends cdk.Stack {
  public readonly database: rds.DatabaseInstance;
  public readonly databaseSecret: secretsmanager.Secret;

  constructor(scope: Construct, id: string, props: DatabaseStackProps) {
    super(scope, id, props);

    // Create database secret
    this.databaseSecret = new secretsmanager.Secret(this, 'DatabaseSecret', {
      secretName: `swvtt/${props.environment}/database`,
      description: 'Database credentials for SWVTT application',
      generateSecretString: {
        secretStringTemplate: JSON.stringify({ username: 'swvttadmin' }),
        generateStringKey: 'password',
        excludeCharacters: '"@/\\',
      },
    });

    // Create database subnet group
    const subnetGroup = new rds.SubnetGroup(this, 'DatabaseSubnetGroup', {
      description: 'Subnet group for SWVTT database',
      vpc: props.vpc,
      vpcSubnets: {
        subnetType: ec2.SubnetType.PRIVATE_ISOLATED,
      },
    });

    // Create database security group
    const databaseSG = new ec2.SecurityGroup(this, 'DatabaseSecurityGroup', {
      vpc: props.vpc,
      description: 'Security group for SWVTT database',
      allowAllOutbound: false,
    });

    // Allow inbound connections on MySQL/Aurora port from application subnets
    databaseSG.addIngressRule(
      ec2.Peer.ipv4('10.0.0.0/16'), // Default VPC CIDR
      ec2.Port.tcp(3306),
      'Allow MySQL connections from VPC'
    );

    // Create RDS instance
    this.database = new rds.DatabaseInstance(this, 'SWVTTDatabase', {
      engine: rds.DatabaseInstanceEngine.mysql({
        version: rds.MysqlEngineVersion.VER_8_0,
      }),
      instanceType: ec2.InstanceType.of(
        ec2.InstanceClass.T3,
        props.instanceClass === 'production' ? ec2.InstanceSize.MEDIUM : ec2.InstanceSize.MICRO
      ),
      vpc: props.vpc,
      subnetGroup,
      securityGroups: [databaseSG],
      credentials: rds.Credentials.fromSecret(this.databaseSecret),
      databaseName: props.databaseName || 'swvtt',
      backupRetention: cdk.Duration.days(props.environment === 'prod' ? 30 : 7),
      deletionProtection: props.environment === 'prod',
      storageEncrypted: true,
      multiAz: props.environment === 'prod',
      allocatedStorage: props.environment === 'prod' ? 100 : 20,
      storageType: rds.StorageType.GP2,
      allowMajorVersionUpgrade: false,
      autoMinorVersionUpgrade: true,
      deleteAutomatedBackups: props.environment !== 'prod',
    });

    // Tag resources appropriately
    cdk.Tags.of(this).add('Project', 'SWVTT');
    cdk.Tags.of(this).add('Environment', props.environment);
    cdk.Tags.of(this).add('Stack', 'Database');

    // Export database information for other stacks
    new cdk.CfnOutput(this, 'DatabaseEndpoint', {
      value: this.database.instanceEndpoint.hostname,
      exportName: `${props.environment}-swvtt-database-endpoint`,
      description: 'Database endpoint for SWVTT application',
    });

    new cdk.CfnOutput(this, 'DatabasePort', {
      value: this.database.instanceEndpoint.port.toString(),
      exportName: `${props.environment}-swvtt-database-port`,
      description: 'Database port for SWVTT application',
    });

    new cdk.CfnOutput(this, 'DatabaseSecretArn', {
      value: this.databaseSecret.secretArn,
      exportName: `${props.environment}-swvtt-database-secret-arn`,
      description: 'Database secret ARN for SWVTT application',
    });
  }
}