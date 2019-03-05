import React from "react";
import { render, fireEvent } from "react-testing-library";
import Graph from "../Graph";
import graphData from "../__mocks__/graphData";

const graphProps = {
  ...graphData,
  edit: false,
  initialDrawMode: "draw-node",
  initialOnDraw: false,
  start_blank: false
};

describe("Node", () => {
  it("should render Node component properly with proper course code", () => {
    const { getByText } = render(<Graph {...graphProps} />);
    const courseTextNode = getByText("CSC104");
    // Check if svg container id is consistent with course code
    expect(courseTextNode.parentNode.id.toUpperCase()).toBe(courseTextNode.innerHTML);
    // TODO: Check if it has a sibling rect element?
  });

  it("should should create an info box when hovering over the course", () => {
    const { getByText } = render(<Graph {...graphProps} />);
    const courseNode = getByText("CSC104").parentNode;
    // Ensure mouse is not hovering over the course
    fireEvent.mouseOut(courseNode);
    // Is the course takeable?
    expect(courseNode.classList.contains("takeable")).toBe(true);
    // Hover over the course
    fireEvent.mouseOver(courseNode);
    // Is the course missing?
    expect(courseNode.classList.contains("missing")).toBe(true);
    let infoNode = getByText("Info");
    // Does the infoBox appear?
    expect(infoNode).toBeDefined();
    // Does the infoBox correspond to the correct course?
    expect(infoNode.id).toEqual("csc104-tooltip-text");
    // Unhover over the course
    fireEvent.mouseOut(courseNode);
    // Is the course takeable again? Did the infoBox disappear?
    expect(courseNode.classList.contains("takeable")).toBe(true);
    // TODO: Asynchrony: mock and wait.
    // infoNode = getByText("Info")
    // expect(infoNode).not.toBeDefined()
  });

  it("should sets attributes correctly when clicking on course", () => {
    const { container, getByText, queryByText } = render(<Graph {...graphProps} />);
    const courseNode = getByText("CSC104").parentNode
    // Ensure mouse is not hovering over the course
    fireEvent.mouseOut(courseNode);
    // Is the course takeable?
    expect(courseNode.classList.contains("takeable")).toBe(true);
    // Click on the course.
    // TODO: redundant to check whether it is missing before clicking?
    fireEvent.click(courseNode);
    // Is the course active?
    // TODO: mock callback and check whether it is called with correct arguments
    expect(courseNode.classList.contains("active")).toBe(true);
    // const nodeOnClick - jest.fn()
    // expect(nodeOnClick).toHaveBeenCalledTimes(1)
    // Click the course again
    fireEvent.click(courseNode);
    // Was the course deselected and once again takeable.
    expect(courseNode.classList.contains("takeable")).toBe(true);
  });
});
