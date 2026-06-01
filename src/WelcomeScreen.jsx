import { useState } from 'react'
import {
  Card,
  CardAction,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card"
import PaginationComponent  from './PaginationComponent.jsx'

export default function WelcomeScreen( props) {
    const {claims, handleLogout} = props
    
    const [activePage, setActivePage] = useState(1)
    const recordsPerPage=10
    const plotPointList = Array.from({length: 100},(_, index) => `Plot Point ${index +1}`)
    const totalPages=Math.round(plotPointList.length/recordsPerPage)
    return (
        <Card className="w-full max-w-md">
            <CardHeader>
                <CardTitle>Plot Points</CardTitle>
            </CardHeader>
            <CardContent>
                <ul>
                    {
                        plotPointList
                            .slice((activePage-1)*recordsPerPage, (((activePage-1)*recordsPerPage)+recordsPerPage))
                            .map((pp, i) => ( <li key={i}>{pp}</li>))
                    }
                </ul>
            </CardContent>
            <CardFooter className="flex justify-center w-full">
                <PaginationComponent activePage={activePage} setActivePage={setActivePage} totalPages={totalPages} recordsPerPage={recordsPerPage} />
            </CardFooter>
        </Card>
    )
}
