# Sequence Diagrams

This document contains sequence diagrams for key workflows in the Savage Worlds Virtual Table Top application.

## 1. Create Plot Point Workflow

```mermaid
sequenceDiagram
    participant U as User
    participant B as Browser
    participant R as React App
    participant RR as React Router
    participant PF as PlotPointForm
    participant A as AWS Amplify
    participant G as GraphQL API
    participant D as DynamoDB

    U->>B: Navigate to /plot_point/add
    B->>R: Load React App
    R->>RR: Route to PlotPointAdd
    RR->>PF: Render PlotPointForm
    PF->>PF: Initialize empty state
    
    U->>PF: Fill in plot point details
    PF->>PF: Update component state
    
    U->>PF: Click Save
    PF->>A: API.graphql(createPlotPoint)
    A->>G: Execute createPlotPoint mutation
    G->>D: Insert plot point record
    D-->>G: Success response
    G-->>A: Return created plot point
    A-->>PF: Plot point created
    PF->>RR: Navigate to plot point list
    RR->>B: Redirect to home page
```

## 2. List Plot Points Workflow

```mermaid
sequenceDiagram
    participant U as User
    participant B as Browser
    participant R as React App
    participant PL as PlotPointList
    participant A as AWS Amplify
    participant G as GraphQL API
    participant D as DynamoDB

    U->>B: Navigate to home (/)
    B->>R: Load React App
    R->>PL: Render PlotPointList
    PL->>PL: componentDidMount()
    PL->>A: API.graphql(listPlotPoints)
    A->>G: Execute listPlotPoints query
    G->>D: Query all plot points
    D-->>G: Return plot points array
    G-->>A: Return query results
    A-->>PL: Plot points data
    PL->>PL: setState with plot points
    PL->>B: Render plot points table
    
    Note over PL: User can click Edit/Delete
    U->>PL: Click Edit button
    PL->>B: Navigate to /plot_point/:name/edit
```

## 3. Edit Plot Point Workflow

```mermaid
sequenceDiagram
    participant U as User
    participant B as Browser
    participant PE as PlotPointEdit
    participant PF as PlotPointForm
    participant A as AWS Amplify
    participant G as GraphQL API

    U->>B: Navigate to /plot_point/:name/edit
    B->>PE: Load PlotPointEdit component
    PE->>PE: Extract name from URL params
    PE->>A: API.graphql(getPlotPoint)
    A->>G: Execute getPlotPoint query
    G-->>A: Return plot point data
    A-->>PE: Plot point details
    PE->>PF: Render form with data
    
    U->>PF: Modify plot point fields
    PF->>PF: Update component state
    
    U->>PF: Click Save
    PF->>A: API.graphql(updatePlotPoint)
    A->>G: Execute updatePlotPoint mutation
    G-->>A: Return updated plot point
    A-->>PF: Update successful
    PF->>B: Navigate to plot point list
```

## 4. Authentication Flow (Currently Disabled)

```mermaid
sequenceDiagram
    participant U as User
    participant B as Browser
    participant A as App Component
    participant AM as Amplify
    participant C as Cognito
    participant WA as withAuthenticator HOC

    Note over A: Authentication currently commented out
    
    U->>B: Access application
    B->>A: Load App.js
    A->>WA: Would wrap with withAuthenticator
    WA->>AM: Check authentication status
    AM->>C: Verify user session
    
    alt User not authenticated
        C-->>AM: No valid session
        AM-->>WA: Show login screen
        WA->>U: Display login form
        U->>WA: Enter credentials
        WA->>AM: Auth.signIn()
        AM->>C: Authenticate user
        C-->>AM: Return tokens
        AM-->>WA: Authentication successful
    else User authenticated
        C-->>AM: Valid session
        AM-->>WA: User authenticated
    end
    
    WA->>A: Render main app
    A->>B: Display application
```

## 5. Cucumber BDD Test Execution

```mermaid
sequenceDiagram
    participant T as Test Runner
    participant C as Cucumber
    participant W as World (Support)
    participant S as Selenium WebDriver
    participant B as Chrome Browser
    participant A as AWS Amplify
    participant R as React App

    T->>C: npm run test:ui
    C->>W: Load world.js
    W->>A: Configure Amplify
    
    C->>C: Parse .feature files
    C->>S: Before hook - setup WebDriver
    S->>B: Launch Chrome browser
    
    loop For each scenario
        C->>W: Execute Given steps
        W->>A: Auth.signIn(test credentials)
        A-->>W: Authentication successful
        W->>S: Navigate to application
        S->>B: Load React app
        
        C->>W: Execute When steps
        W->>S: Interact with page elements
        S->>B: Fill forms/click buttons
        B->>R: Update React state
        
        C->>W: Execute Then steps
        W->>S: Assert page state
        S->>B: Read page content
        B-->>S: Return element values
        S-->>W: Assertion results
        
        alt Test fails
            W->>S: Take screenshot
            S->>B: Capture page
            B-->>S: Screenshot data
            S-->>W: Save screenshot
        end
        
        C->>W: After hook - cleanup
        W->>A: Delete test data
    end
    
    C->>S: Quit WebDriver
    S->>B: Close browser
    C->>T: Return test results
```

## 6. GraphQL Subscription Flow (Real-time Updates)

```mermaid
sequenceDiagram
    participant C1 as Client 1
    participant C2 as Client 2
    participant A as AWS Amplify
    participant AS as AppSync
    participant WS as WebSocket
    participant D as DynamoDB

    Note over C1,C2: Both clients viewing plot points
    
    C1->>A: Subscribe to onCreatePlotPoint
    A->>AS: Establish GraphQL subscription
    AS->>WS: Open WebSocket connection
    WS-->>C1: Subscription confirmed
    
    C2->>A: Subscribe to onCreatePlotPoint
    A->>AS: Establish GraphQL subscription
    AS->>WS: Open WebSocket connection
    WS-->>C2: Subscription confirmed
    
    C1->>A: Create new plot point
    A->>AS: Execute createPlotPoint mutation
    AS->>D: Insert record
    D-->>AS: Success
    AS->>WS: Broadcast update
    
    par Notify Client 1
        WS-->>C1: New plot point data
        C1->>C1: Update UI
    and Notify Client 2
        WS-->>C2: New plot point data
        C2->>C2: Update UI
    end
```

## Key Workflow Patterns

1. **Component Lifecycle**: Most data fetching happens in `componentDidMount()` for class components
2. **State Management**: Direct component state updates using `setState()`
3. **API Calls**: All GraphQL operations go through AWS Amplify's `API.graphql()`
4. **Navigation**: React Router handles client-side routing
5. **Error Handling**: Currently minimal - most workflows assume success
6. **Authentication**: Designed to use AWS Cognito but currently disabled