import React from "react";
import { shallow, mount } from "enzyme";
import graphData from "../__mocks__/graphData";
import Graph from "../Graph";

let graph;

beforeAll(() => {
    const graphProps = {
        ...graphData,
        edit: false,
        initialDrawMode: "draw-node",
        initialOnDraw: false,
        start_blank: false
    };
    graph = mount(<Graph {...graphProps} />);
});

describe("Enzyme vs RTF Tests with CSC104", () => {
    it("should render Node component properly with proper course code", () => {
        const nodes = graph.find("Node");
        const csc104 = nodes.findWhere(node => node.key() === 'csc104');

        // some ways to check that CSC104 exists as expected
        expect(Array.isArray(csc104.prop('parents'))).toBe(true);
        expect(csc104.prop('parents').length).toBe(0);
        const g104 = csc104.find("g#csc104");
        expect(g104.containsAllMatchingElements([
            <rect height={30} width={65} rx="4" ry="4" x={674.184078} y={38.09390799999999} style={{}} />,
            <text x={706.684078} y={59.379107999999995}>,
                CSC104
            </text>
        ]));
    });

    it("should should create an info box when hovering over the course", () => {

        const csc104 = graph.find("g#csc104")
        csc104.simulate("click");
    //     TypeError: Cannot read property 'state' of undefined

    // Graph.js
    //     209 |         var courseId = event.currentTarget.id;
    //     210 |         var currentNode = this.refs.nodes.refs[courseId];
    // > 211 |         var wasSelected = currentNode.state.selected;
    //       |                                       ^
    //         212 | currentNode.toggleSelection(this);
    //     213 |         if (wasSelected) {
    //         214 |             // TODO: Differenti        
    });

    it("should should create an info box when hovering over the course", () => {
        // same problem as clicking
        const nodes = graph.find("Node");
        const csc104 = nodes.findWhere(node => node.key() === 'csc104');

        csc104.simulate("mouseover");

        expect(graph.containsAllMatchingElements([
            <g id="infoBox" className="tooltip-group">,
                <rect id="csc104-tooltip-rect" x={609.184078} y={38.0939} rx="4" ry="4" fill="white" stroke="black" strokeWidth="2" width="60" height="30"></rect>,
                <text id="csc104-tooltip-text" x={621.184} y={59.09}>Info</text>,
            </g>
        ]));
    });

    it("should show an info modal when hovering over the course", () => {
        // same problem as clicking
        const csc104 = graph.find("g#csc104")
        csc104.simulate("mouseover");
        expect(graph.containsAllMatchingElements([
            <rect id="csc104-tooltip-rect" x={609.184078} y={38.0939} rx="4" ry="4" fill="white" stroke="black" strokeWidth="2" width="60" height="30"></rect>,
            <text id="csc104-tooltip-text" x={621.184} y={59.09}>Info</text>
        ]));
    });

    it("should sets attributes correctly when clicking on course", () => {
        const nodes = graph.find("Node");
        const csc104 = nodes.findWhere(node => node.key() === 'csc104');
        csc104.simulate("click");
        // ERROR! Invariant Violation: Unable to find node on an unmounted component.
        // csc104.mount() (ERROR! needs to be the root)
    });

});
